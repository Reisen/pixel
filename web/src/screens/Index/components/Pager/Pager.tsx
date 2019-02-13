import IconButton from '../../../../components/IconButton';
import React      from 'react';
import styles     from './Pager.module.css';

interface Props {
    current: number;
    total: number;
}

const Pager = (props: Props) => (
    <div className={styles.Pager}>
        <div className={styles.IconRow}>
            <IconButton icon="1"/>
            <IconButton active icon="2"/>
            <IconButton icon="3"/>
            <IconButton icon="4"/>
            <IconButton icon="5"/>
            <IconButton icon="6"/>
        </div>

        <div className={styles.IconRow}>
            <IconButton icon="<"/>
            <IconButton icon=">"/>
        </div>
    </div>
);

export default Pager;
