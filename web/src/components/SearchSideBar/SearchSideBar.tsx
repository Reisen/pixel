import React, { useState }  from 'react';

import TextInput            from '../TextInput';
import IconButton           from '../IconButton';
import styles               from './SearchSideBar.module.css';


interface Props {
    initialPanel: string;
    children?: {
        icon: string;
        name: string;
        elem: JSX.Element
    }[];
}

const SearchSideBar = (props: Props) => {
    const [panel, changePanel] = useState<string>(props.initialPanel);
    const current              = props.children && props.children.find(child => child.name === panel)
    const pannelButtons        = props.children && props.children.map(child =>
        <IconButton
            key={child.name}
            icon={child.icon}
            active={panel === child.name}
            onClick={() => changePanel(child.name)}
        />
    )

    return (
        <div className={styles.Root}>
            <TextInput placeholder="Search Tags" />

            { current && current.elem }
        </div>
    );
}

export default SearchSideBar;
