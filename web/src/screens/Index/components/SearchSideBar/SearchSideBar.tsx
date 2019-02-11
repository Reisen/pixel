import React     from 'react';
import TextInput from '../../../../components/TextInput'
import styles    from './SearchSideBar.module.css';

/* -------------------------------------------------------------------------- */

interface TagProps {
    tag: string;
}

const Tag = (props: TagProps) => (
    <a href="#">
        {props.tag}
    </a>
);

/* -------------------------------------------------------------------------- */

interface Props {
    tags?: string[];
}

const SearchSideBar = (props: Props) => (
    <div className={styles.SearchSideBar}>
        <TextInput placeholder="Search" />

        <div className={styles.TagList}>
            {
                !props.tags ? null : props.tags.map(tag => (
                    <a href="#">
                        # { tag }
                    </a>
                ))
            }
        </div>
    </div>
);

export default SearchSideBar;
